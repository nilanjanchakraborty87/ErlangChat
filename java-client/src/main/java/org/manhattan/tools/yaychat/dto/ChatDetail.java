package org.manhattan.tools.yaychat.dto;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@lombok.Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonIgnoreProperties({"recordType"})
public class ChatDetail extends Record{
    @JsonProperty("__rec")
    final String __rec = "chat_detail";
    private long from;
    private long to;
    private String message;

    @Override
    public String getRecordType() {
        return "chat";
    }
}
