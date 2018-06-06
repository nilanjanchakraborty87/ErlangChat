package org.manhattan.tools.yaychat.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonSetter;
import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonPropertyOrder({"name", "mobile", "email", "password"})
public class Registration extends Record{
    @JsonProperty("__rec")
    final String __rec = "registration";
    private String name;
    private long mobile;
    @JsonProperty("emailId")
    private String email;
    private String password;

    @Override
    @JsonProperty("__rec")
    public String getRecordType() {
        return __rec;
    }
}
